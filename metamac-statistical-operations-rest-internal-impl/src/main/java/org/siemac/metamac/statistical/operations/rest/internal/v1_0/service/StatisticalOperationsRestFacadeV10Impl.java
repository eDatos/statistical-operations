package org.siemac.metamac.statistical.operations.rest.internal.v1_0.service;

import java.util.ArrayList;
import java.util.List;

import javax.ws.rs.core.Context;
import javax.ws.rs.core.Response.Status;

import org.apache.cxf.jaxrs.ext.MessageContext;
import org.fornax.cartridges.sculptor.framework.accessapi.ConditionalCriteria;
import org.fornax.cartridges.sculptor.framework.accessapi.ConditionalCriteriaBuilder;
import org.fornax.cartridges.sculptor.framework.domain.PagedResult;
import org.fornax.cartridges.sculptor.framework.domain.PagingParameter;
import org.fornax.cartridges.sculptor.framework.errorhandling.ServiceContext;
import org.siemac.metamac.core.common.aop.LoggingInterceptor;
import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.rest.common.v1_0.domain.Error;
import org.siemac.metamac.rest.common.v1_0.domain.ResourcesNoPagedResult;
import org.siemac.metamac.rest.common.v1_0.domain.ResourcesPagedResult;
import org.siemac.metamac.rest.exception.RestException;
import org.siemac.metamac.rest.exception.utils.RestExceptionUtils;
import org.siemac.metamac.rest.search.criteria.SculptorCriteria;
import org.siemac.metamac.rest.utils.RestUtils;
import org.siemac.metamac.statistical.operations.core.domain.FamilyProperties;
import org.siemac.metamac.statistical.operations.core.domain.InstanceProperties;
import org.siemac.metamac.statistical.operations.core.domain.OperationProperties;
import org.siemac.metamac.statistical.operations.core.enume.domain.ProcStatusEnum;
import org.siemac.metamac.statistical.operations.core.serviceapi.StatisticalOperationsBaseService;
import org.siemac.metamac.statistical.operations.rest.internal.exception.RestServiceExceptionType;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Family;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Instance;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.domain.Operation;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.mapper.Do2RestInternalMapperV10;
import org.siemac.metamac.statistical.operations.rest.internal.v1_0.mapper.RestCriteria2SculptorCriteriaMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service("statisticalOperationsRestFacadeV10")
public class StatisticalOperationsRestFacadeV10Impl implements StatisticalOperationsRestFacadeV10 {

    @Autowired
    private StatisticalOperationsBaseService    statisticalOperationsBaseService;

    @Autowired
    private Do2RestInternalMapperV10            do2RestInternalMapper;

    @Autowired
    private RestCriteria2SculptorCriteriaMapper restCriteria2SculptorCriteriaMapper;

    // @Context // must inject with setter, because with @Context is not injected in web server
    private MessageContext                      context;

    private ServiceContext                      serviceContextRestInternal = new ServiceContext("restInternal", "restInternal", "restInternal");
    private Logger                              logger                     = LoggerFactory.getLogger(LoggingInterceptor.class);

    @Context
    public void setMessageContext(MessageContext context) {
        this.context = context;
    }

    @Override
    public Operation retrieveOperationById(String id) {
        try {
            // Retrieve
            org.siemac.metamac.statistical.operations.core.domain.Operation operationEntity = retrieveOperationEntityPublishedInternalOrExternally(id);

            // Transform
            Operation operation = do2RestInternalMapper.toOperation(operationEntity, getApiUrl());
            return operation;

        } catch (MetamacException e) {
            throw manageException(e);
        }
    }

    @Override
    public ResourcesPagedResult findOperations(String query, String orderBy, String limit, String offset) {
        try {
            // Retrieve operations by criteria
            SculptorCriteria sculptorCriteria = restCriteria2SculptorCriteriaMapper.getOperationCriteriaMapper().restCriteriaToSculptorCriteria(query, orderBy, limit, offset);
            // Find only published
            ConditionalCriteria conditionalCriteriaProcStatus = ConditionalCriteriaBuilder.criteriaFor(org.siemac.metamac.statistical.operations.core.domain.Operation.class)
                    .withProperty(OperationProperties.procStatus()).in(ProcStatusEnum.PUBLISH_INTERNALLY, ProcStatusEnum.PUBLISH_EXTERNALLY).buildSingle();

            List<ConditionalCriteria> conditionalCriteria = new ArrayList<ConditionalCriteria>();
            conditionalCriteria.add(conditionalCriteriaProcStatus);
            conditionalCriteria.addAll(sculptorCriteria.getConditions());

            // Retrieve
            PagedResult<org.siemac.metamac.statistical.operations.core.domain.Operation> operationsEntitiesResult = statisticalOperationsBaseService.findOperationByCondition(
                    serviceContextRestInternal, conditionalCriteria, sculptorCriteria.getPagingParameter());

            // Transform
            ResourcesPagedResult operationsPagedResult = do2RestInternalMapper.toOperationsPagedResult(operationsEntitiesResult, query, orderBy, sculptorCriteria.getLimit(), getApiUrl());
            return operationsPagedResult;

        } catch (MetamacException e) {
            throw manageException(e);
        }
    }

    // TODO parámetro "query" con criterios de búsqueda METAMAC-753
    @Override
    public ResourcesPagedResult findInstances(String operationId, String limit, String offset) {
        try {
            // Retrieve operation to check exists and it is published
            org.siemac.metamac.statistical.operations.core.domain.Operation operationEntity = retrieveOperationEntityPublishedInternalOrExternally(operationId);

            // Retrieve instances by criteria
            SculptorCriteria sculptorCriteria = null; // TODO: RestCriteria2SculptorCriteria.restCriteriaToSculptorCriteria(limit, offset);
            // Find for operation and only published
            List<ConditionalCriteria> conditionalCriteria = ConditionalCriteriaBuilder.criteriaFor(org.siemac.metamac.statistical.operations.core.domain.Instance.class)
                    .withProperty(InstanceProperties.operation().code()).eq(operationId).withProperty(InstanceProperties.procStatus())
                    .in(ProcStatusEnum.PUBLISH_INTERNALLY, ProcStatusEnum.PUBLISH_EXTERNALLY).distinctRoot().build();
            conditionalCriteria.addAll(sculptorCriteria.getConditions());

            // Retrieve
            PagedResult<org.siemac.metamac.statistical.operations.core.domain.Instance> instancesEntitiesResult = statisticalOperationsBaseService.findInstanceByCondition(serviceContextRestInternal,
                    conditionalCriteria, sculptorCriteria.getPagingParameter());

            // Transform
            ResourcesPagedResult instancesPagedResult = do2RestInternalMapper.toInstancesPagedResult(operationEntity, instancesEntitiesResult, null, null, sculptorCriteria.getLimit(), getApiUrl());
            return instancesPagedResult;

        } catch (MetamacException e) {
            throw manageException(e);
        }
    }

    @Override
    public ResourcesNoPagedResult retrieveFamiliesByOperation(String id) {
        try {
            // Retrieve operation to check exists and it is published
            retrieveOperationEntityPublishedInternalOrExternally(id);

            // Retrieve families by criteria
            List<ConditionalCriteria> conditionalCriteria = ConditionalCriteriaBuilder.criteriaFor(org.siemac.metamac.statistical.operations.core.domain.Family.class)
                    .withProperty(FamilyProperties.operations().code()).eq(id).withProperty(FamilyProperties.procStatus()).in(ProcStatusEnum.PUBLISH_INTERNALLY, ProcStatusEnum.PUBLISH_EXTERNALLY)
                    .distinctRoot().build();
            PagingParameter pagingParameter = PagingParameter.noLimits();
            PagedResult<org.siemac.metamac.statistical.operations.core.domain.Family> familiesEntitiesResult = statisticalOperationsBaseService.findFamilyByCondition(serviceContextRestInternal,
                    conditionalCriteria, pagingParameter);

            // Transform
            ResourcesNoPagedResult familiesNoPagedResult = do2RestInternalMapper.toFamiliesByOperationNoPagedResult(familiesEntitiesResult.getValues(), getApiUrl());
            return familiesNoPagedResult;

        } catch (MetamacException e) {
            throw manageException(e);
        }
    }

    @Override
    public Family retrieveFamilyById(String id) {
        try {
            // Retrieve
            org.siemac.metamac.statistical.operations.core.domain.Family familyEntity = retrieveFamilyEntityPublishedInternalOrExternally(id);

            // Transform
            Family family = do2RestInternalMapper.toFamily(familyEntity, getApiUrl());
            return family;

        } catch (MetamacException e) {
            throw manageException(e);
        }
    }

    @Override
    public ResourcesPagedResult findFamilies(String query, String orderBy, String limit, String offset) {
        try {
            // Retrieve families by criteria
            SculptorCriteria sculptorCriteria = restCriteria2SculptorCriteriaMapper.getFamilyCriteriaMapper().restCriteriaToSculptorCriteria(query, orderBy, limit, offset);
            // Find only published
            ConditionalCriteria conditionalCriteriaProcStatus = ConditionalCriteriaBuilder.criteriaFor(org.siemac.metamac.statistical.operations.core.domain.Family.class)
                    .withProperty(FamilyProperties.procStatus()).in(ProcStatusEnum.PUBLISH_INTERNALLY, ProcStatusEnum.PUBLISH_EXTERNALLY).buildSingle();

            List<ConditionalCriteria> conditionalCriteria = new ArrayList<ConditionalCriteria>();
            conditionalCriteria.add(conditionalCriteriaProcStatus);
            conditionalCriteria.addAll(sculptorCriteria.getConditions());

            // Retrieve
            PagedResult<org.siemac.metamac.statistical.operations.core.domain.Family> familiesEntitiesResult = statisticalOperationsBaseService.findFamilyByCondition(serviceContextRestInternal,
                    conditionalCriteria, sculptorCriteria.getPagingParameter());

            // Transform
            ResourcesPagedResult familiesPagedResult = do2RestInternalMapper.toFamiliesPagedResult(familiesEntitiesResult, query, orderBy, sculptorCriteria.getLimit(), getApiUrl());
            return familiesPagedResult;

        } catch (MetamacException e) {
            throw manageException(e);
        }
    }

    @Override
    public ResourcesPagedResult retrieveOperationsByFamily(String id, String limit, String offset) {
        try {
            // Validate family exists and it is published
            org.siemac.metamac.statistical.operations.core.domain.Family family = retrieveFamilyEntityPublishedInternalOrExternally(id);

            // Retrieve operations by criteria
            SculptorCriteria sculptorCriteria = null; // TODO: RestCriteria2SculptorCriteria.restCriteriaToSculptorCriteria(limit, offset);
            // Find only this family and published
            List<ConditionalCriteria> conditionalCriteria = ConditionalCriteriaBuilder.criteriaFor(org.siemac.metamac.statistical.operations.core.domain.Operation.class)
                    .withProperty(OperationProperties.families().code()).eq(id).withProperty(OperationProperties.procStatus())
                    .in(ProcStatusEnum.PUBLISH_INTERNALLY, ProcStatusEnum.PUBLISH_EXTERNALLY).distinctRoot().build();
            conditionalCriteria.addAll(sculptorCriteria.getConditions());

            // Retrieve
            PagedResult<org.siemac.metamac.statistical.operations.core.domain.Operation> operationsEntitiesResult = statisticalOperationsBaseService.findOperationByCondition(
                    serviceContextRestInternal, conditionalCriteria, sculptorCriteria.getPagingParameter());

            // Transform
            ResourcesPagedResult operationsPagedResult = do2RestInternalMapper.toOperationsByFamilyPagedResult(family, operationsEntitiesResult, null, null, sculptorCriteria.getLimit(), getApiUrl());
            return operationsPagedResult;

        } catch (MetamacException e) {
            throw manageException(e);
        }
    }

    @Override
    public Instance retrieveInstanceById(String operationId, String id) {
        try {
            org.siemac.metamac.statistical.operations.core.domain.Instance instanceEntity = statisticalOperationsBaseService.findInstanceByCode(serviceContextRestInternal, id);
            if (instanceEntity == null || (!ProcStatusEnum.PUBLISH_EXTERNALLY.equals(instanceEntity.getProcStatus()) && !ProcStatusEnum.PUBLISH_INTERNALLY.equals(instanceEntity.getProcStatus()))) {
                // Instance not found
                Error error = RestExceptionUtils.getError(RestServiceExceptionType.INSTANCE_NOT_FOUND, id);
                throw new RestException(error, Status.NOT_FOUND);
            }

            // Transform and return
            Instance instance = do2RestInternalMapper.toInstance(instanceEntity, getApiUrl());
            return instance;

        } catch (MetamacException e) {
            throw manageException(e);
        }
    }

    /**
     * Retrieve operation by code (id in Api) and check it is published externally or internally
     */
    private org.siemac.metamac.statistical.operations.core.domain.Operation retrieveOperationEntityPublishedInternalOrExternally(String code) throws MetamacException {
        org.siemac.metamac.statistical.operations.core.domain.Operation operationEntity = statisticalOperationsBaseService.findOperationByCode(serviceContextRestInternal, code);
        if (operationEntity == null || (!ProcStatusEnum.PUBLISH_EXTERNALLY.equals(operationEntity.getProcStatus()) && !ProcStatusEnum.PUBLISH_INTERNALLY.equals(operationEntity.getProcStatus()))) {
            // Operation not found
            Error error = RestExceptionUtils.getError(RestServiceExceptionType.OPERATION_NOT_FOUND, code);
            throw new RestException(error, Status.NOT_FOUND);
        }
        return operationEntity;
    }

    /**
     * Retrieve family by code (id in Api) and check it is published externally or internally
     */
    private org.siemac.metamac.statistical.operations.core.domain.Family retrieveFamilyEntityPublishedInternalOrExternally(String code) throws MetamacException {
        org.siemac.metamac.statistical.operations.core.domain.Family familyEntity = statisticalOperationsBaseService.findFamilyByCode(serviceContextRestInternal, code);

        if (familyEntity == null || (!ProcStatusEnum.PUBLISH_EXTERNALLY.equals(familyEntity.getProcStatus()) && !ProcStatusEnum.PUBLISH_INTERNALLY.equals(familyEntity.getProcStatus()))) {
            // Family not found or not published
            Error error = RestExceptionUtils.getError(RestServiceExceptionType.FAMILY_NOT_FOUND, code);
            throw new RestException(error, Status.NOT_FOUND);
        }
        return familyEntity;
    }

    /**
     * Get Base API url
     */
    private String getApiUrl() {
        return RestUtils.getApiUrl(context);
    }

    /**
     * Throws response error, logging exception
     */
    private RestException manageException(MetamacException e) {
        logger.error("Error", e);
        Error error = do2RestInternalMapper.toError(e);
        return new RestException(error, Status.INTERNAL_SERVER_ERROR);
    }
}