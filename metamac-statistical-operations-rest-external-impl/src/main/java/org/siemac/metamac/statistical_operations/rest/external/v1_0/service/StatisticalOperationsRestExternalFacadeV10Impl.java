package org.siemac.metamac.statistical_operations.rest.external.v1_0.service;

import java.util.ArrayList;
import java.util.List;

import javax.ws.rs.core.Response.Status;

import org.fornax.cartridges.sculptor.framework.accessapi.ConditionalCriteria;
import org.fornax.cartridges.sculptor.framework.accessapi.ConditionalCriteriaBuilder;
import org.fornax.cartridges.sculptor.framework.domain.PagedResult;
import org.fornax.cartridges.sculptor.framework.domain.PagingParameter;
import org.fornax.cartridges.sculptor.framework.errorhandling.ServiceContext;
import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.rest.exception.RestCommonServiceExceptionType;
import org.siemac.metamac.rest.exception.RestException;
import org.siemac.metamac.rest.exception.utils.RestExceptionUtils;
import org.siemac.metamac.rest.search.criteria.SculptorCriteria;
import org.siemac.metamac.rest.statistical_operations.v1_0.domain.CollMethods;
import org.siemac.metamac.rest.statistical_operations.v1_0.domain.Costs;
import org.siemac.metamac.rest.statistical_operations.v1_0.domain.Families;
import org.siemac.metamac.rest.statistical_operations.v1_0.domain.Family;
import org.siemac.metamac.rest.statistical_operations.v1_0.domain.Instance;
import org.siemac.metamac.rest.statistical_operations.v1_0.domain.InstanceTypes;
import org.siemac.metamac.rest.statistical_operations.v1_0.domain.Instances;
import org.siemac.metamac.rest.statistical_operations.v1_0.domain.OfficialityTypes;
import org.siemac.metamac.rest.statistical_operations.v1_0.domain.Operation;
import org.siemac.metamac.rest.statistical_operations.v1_0.domain.Operations;
import org.siemac.metamac.rest.statistical_operations.v1_0.domain.StatisticalOperationSources;
import org.siemac.metamac.rest.statistical_operations.v1_0.domain.StatisticalOperationTypes;
import org.siemac.metamac.statistical.operations.core.domain.CollMethod;
import org.siemac.metamac.statistical.operations.core.domain.Cost;
import org.siemac.metamac.statistical.operations.core.domain.FamilyProperties;
import org.siemac.metamac.statistical.operations.core.domain.InstanceProperties;
import org.siemac.metamac.statistical.operations.core.domain.InstanceType;
import org.siemac.metamac.statistical.operations.core.domain.OfficialityType;
import org.siemac.metamac.statistical.operations.core.domain.OperationProperties;
import org.siemac.metamac.statistical.operations.core.domain.SurveySource;
import org.siemac.metamac.statistical.operations.core.domain.SurveyType;
import org.siemac.metamac.statistical.operations.core.enume.domain.ProcStatusEnum;
import org.siemac.metamac.statistical.operations.core.serviceapi.StatisticalOperationsBaseService;
import org.siemac.metamac.statistical.operations.core.serviceapi.StatisticalOperationsListsService;
import org.siemac.metamac.statistical_operations.rest.common.StatisticalOperationsRestConstants;
import org.siemac.metamac.statistical_operations.rest.external.exception.RestServiceExceptionType;
import org.siemac.metamac.statistical_operations.rest.external.v1_0.mapper.Do2RestExternalMapperV10;
import org.siemac.metamac.statistical_operations.rest.external.v1_0.mapper.RestCriteria2SculptorCriteriaMapper;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

@Service("statisticalOperationsRestExternalFacadeV10")
public class StatisticalOperationsRestExternalFacadeV10Impl implements StatisticalOperationsV1_0 {

    @Autowired
    private StatisticalOperationsBaseService    statisticalOperationsBaseService;

    @Autowired
    private StatisticalOperationsListsService   statisticalOperationsListsService;

    @Autowired
    private Do2RestExternalMapperV10            do2RestExternalMapper;

    @Autowired
    private RestCriteria2SculptorCriteriaMapper restCriteria2SculptorCriteriaMapper;

    private final ServiceContext                serviceContextRestExternal = new ServiceContext("restExternal", "restExternal", "restExternal");
    private final Logger                        logger                     = LoggerFactory.getLogger(StatisticalOperationsRestExternalFacadeV10Impl.class);

    @Override
    public Operation retrieveOperationById(String id) {
        try {
            // Retrieve
            org.siemac.metamac.statistical.operations.core.domain.Operation operationEntity = retrieveOperationEntityPublishedExternally(id);

            // Transform
            Operation operation = do2RestExternalMapper.toOperation(operationEntity);
            return operation;

        } catch (Exception e) {
            throw manageException(e);
        }
    }

    @Override
    public Operations findOperations(String query, String orderBy, String limit, String offset) {
        try {
            // Retrieve operations by criteria
            SculptorCriteria sculptorCriteria = restCriteria2SculptorCriteriaMapper.getOperationCriteriaMapper().restCriteriaToSculptorCriteria(query, orderBy, limit, offset);
            // Find only published
            ConditionalCriteria conditionalCriteriaProcStatus = ConditionalCriteriaBuilder.criteriaFor(org.siemac.metamac.statistical.operations.core.domain.Operation.class)
                    .withProperty(OperationProperties.procStatus()).eq(ProcStatusEnum.PUBLISH_EXTERNALLY).buildSingle();

            List<ConditionalCriteria> conditionalCriteria = new ArrayList<ConditionalCriteria>();
            conditionalCriteria.add(conditionalCriteriaProcStatus);
            conditionalCriteria.addAll(sculptorCriteria.getConditions());

            // Retrieve
            PagedResult<org.siemac.metamac.statistical.operations.core.domain.Operation> operationsEntitiesResult = statisticalOperationsBaseService.findOperationByCondition(
                    serviceContextRestExternal, conditionalCriteria, sculptorCriteria.getPagingParameter());

            // Transform
            Operations operations = do2RestExternalMapper.toOperations(operationsEntitiesResult, query, orderBy, sculptorCriteria.getLimit());
            return operations;

        } catch (Exception e) {
            throw manageException(e);
        }
    }

    @Override
    public Instances findInstances(String operationId, String query, String orderBy, String limit, String offset) {
        try {
            // Retrieve instances by criteria
            SculptorCriteria sculptorCriteria = restCriteria2SculptorCriteriaMapper.getInstanceCriteriaMapper().restCriteriaToSculptorCriteria(query, orderBy, limit, offset);
            List<ConditionalCriteria> conditionalCriteria = new ArrayList<ConditionalCriteria>();

            // Find for operation
            org.siemac.metamac.statistical.operations.core.domain.Operation operationEntity = null;
            if (!StatisticalOperationsRestConstants.WILDCARD_ALL.equals(operationId)) {
                operationEntity = retrieveOperationEntityPublishedExternally(operationId);
                ConditionalCriteria conditionalCriteriaOperationId = ConditionalCriteriaBuilder.criteriaFor(org.siemac.metamac.statistical.operations.core.domain.Instance.class)
                        .withProperty(InstanceProperties.operation().code()).eq(operationId).buildSingle();
                conditionalCriteria.add(conditionalCriteriaOperationId);
            }
            // Find only instances externally published
            ConditionalCriteria conditionalCriteriaPublished = ConditionalCriteriaBuilder.criteriaFor(org.siemac.metamac.statistical.operations.core.domain.Instance.class)
                    .withProperty(InstanceProperties.procStatus()).eq(ProcStatusEnum.PUBLISH_EXTERNALLY).buildSingle();
            conditionalCriteria.add(conditionalCriteriaPublished);

            // Conditions requested
            conditionalCriteria.addAll(sculptorCriteria.getConditions());

            // Retrieve
            PagedResult<org.siemac.metamac.statistical.operations.core.domain.Instance> instancesEntitiesResult = statisticalOperationsBaseService.findInstanceByCondition(serviceContextRestExternal,
                    conditionalCriteria, sculptorCriteria.getPagingParameter());

            // Transform
            Instances instances = do2RestExternalMapper.toInstances(operationEntity, instancesEntitiesResult, query, orderBy, sculptorCriteria.getLimit());
            return instances;
        } catch (Exception e) {
            throw manageException(e);
        }
    }

    @Override
    public Families retrieveFamiliesByOperation(String id) {
        try {
            // Retrieve operation to check exists and it is published
            retrieveOperationEntityPublishedExternally(id);

            // Retrieve families by criteria
            List<ConditionalCriteria> conditionalCriteria = ConditionalCriteriaBuilder.criteriaFor(org.siemac.metamac.statistical.operations.core.domain.Family.class)
                    .withProperty(FamilyProperties.operations().code()).eq(id).withProperty(FamilyProperties.procStatus()).eq(ProcStatusEnum.PUBLISH_EXTERNALLY).distinctRoot().build();
            PagingParameter pagingParameter = PagingParameter.noLimits();
            PagedResult<org.siemac.metamac.statistical.operations.core.domain.Family> familiesEntitiesResult = statisticalOperationsBaseService.findFamilyByCondition(serviceContextRestExternal,
                    conditionalCriteria, pagingParameter);

            // Transform
            Families families = do2RestExternalMapper.toFamiliesByOperation(familiesEntitiesResult.getValues());
            return families;

        } catch (Exception e) {
            throw manageException(e);
        }
    }

    @Override
    public Family retrieveFamilyById(String id) {
        try {
            // Retrieve
            org.siemac.metamac.statistical.operations.core.domain.Family familyEntity = retrieveFamilyEntityPublishedExternally(id);

            // Transform
            Family family = do2RestExternalMapper.toFamily(familyEntity);
            return family;

        } catch (Exception e) {
            throw manageException(e);
        }
    }

    @Override
    public Families findFamilies(String query, String orderBy, String limit, String offset) {
        try {
            // Retrieve families by criteria
            SculptorCriteria sculptorCriteria = restCriteria2SculptorCriteriaMapper.getFamilyCriteriaMapper().restCriteriaToSculptorCriteria(query, orderBy, limit, offset);
            // Find only published
            ConditionalCriteria conditionalCriteriaProcStatus = ConditionalCriteriaBuilder.criteriaFor(org.siemac.metamac.statistical.operations.core.domain.Family.class)
                    .withProperty(FamilyProperties.procStatus()).eq(ProcStatusEnum.PUBLISH_EXTERNALLY).buildSingle();

            List<ConditionalCriteria> conditionalCriteria = new ArrayList<ConditionalCriteria>();
            conditionalCriteria.add(conditionalCriteriaProcStatus);
            conditionalCriteria.addAll(sculptorCriteria.getConditions());

            // Retrieve
            PagedResult<org.siemac.metamac.statistical.operations.core.domain.Family> familiesEntitiesResult = statisticalOperationsBaseService.findFamilyByCondition(serviceContextRestExternal,
                    conditionalCriteria, sculptorCriteria.getPagingParameter());

            // Transform
            Families families = do2RestExternalMapper.toFamilies(familiesEntitiesResult, query, orderBy, sculptorCriteria.getLimit());
            return families;

        } catch (Exception e) {
            throw manageException(e);
        }
    }

    @Override
    public Operations findOperationsByFamily(String id, String query, String orderBy, String limit, String offset) {
        try {
            // Validate family exists and it is published
            org.siemac.metamac.statistical.operations.core.domain.Family family = retrieveFamilyEntityPublishedExternally(id);

            // Retrieve operations by criteria
            SculptorCriteria sculptorCriteria = restCriteria2SculptorCriteriaMapper.getOperationCriteriaMapper().restCriteriaToSculptorCriteria(query, orderBy, limit, offset);
            // Find only this family and published
            List<ConditionalCriteria> conditionalCriteria = ConditionalCriteriaBuilder.criteriaFor(org.siemac.metamac.statistical.operations.core.domain.Operation.class)
                    .withProperty(OperationProperties.families().code()).eq(id).withProperty(OperationProperties.procStatus()).eq(ProcStatusEnum.PUBLISH_EXTERNALLY).build();
            conditionalCriteria.addAll(sculptorCriteria.getConditions());

            // Retrieve
            PagedResult<org.siemac.metamac.statistical.operations.core.domain.Operation> operationsEntitiesResult = statisticalOperationsBaseService.findOperationByCondition(
                    serviceContextRestExternal, conditionalCriteria, sculptorCriteria.getPagingParameter());

            // Transform
            Operations operations = do2RestExternalMapper.toOperationsByFamily(family, operationsEntitiesResult, query, orderBy, sculptorCriteria.getLimit());
            return operations;

        } catch (Exception e) {
            throw manageException(e);
        }
    }

    @Override
    public Instance retrieveInstanceById(String operationId, String id) {
        try {
            org.siemac.metamac.statistical.operations.core.domain.Instance instanceEntity = statisticalOperationsBaseService.findInstanceByCode(serviceContextRestExternal, id);
            if (instanceEntity == null || !ProcStatusEnum.PUBLISH_EXTERNALLY.equals(instanceEntity.getProcStatus())) {
                // Instance not found
                org.siemac.metamac.rest.common.v1_0.domain.Exception exception = RestExceptionUtils.getException(RestServiceExceptionType.INSTANCE_NOT_FOUND, id);
                throw new RestException(exception, Status.NOT_FOUND);
            }

            // Transform and return
            Instance instance = do2RestExternalMapper.toInstance(instanceEntity);
            return instance;

        } catch (Exception e) {
            throw manageException(e);
        }
    }

    @Override
    public StatisticalOperationTypes retrieveStatisticalOperationTypes() {
        try {
            // Retrieve all
            List<SurveyType> entitiesResult = statisticalOperationsListsService.findAllSurveyTypes(serviceContextRestExternal);

            // Transform
            StatisticalOperationTypes statisticalOperationTypes = do2RestExternalMapper.toStatisticalOperationTypes(entitiesResult);
            return statisticalOperationTypes;
        } catch (Exception e) {
            throw manageException(e);
        }
    }

    @Override
    public OfficialityTypes retrieveOfficialityTypes() {
        try {
            // Retrieve all
            List<OfficialityType> entitiesResult = statisticalOperationsListsService.findAllOfficialityTypes(serviceContextRestExternal);

            // Transform
            OfficialityTypes officialityTypes = do2RestExternalMapper.toOfficialityTypes(entitiesResult);
            return officialityTypes;
        } catch (Exception e) {
            throw manageException(e);
        }
    }

    @Override
    public InstanceTypes retrieveInstanceTypes() {
        try {
            // Retrieve all
            List<InstanceType> entitiesResult = statisticalOperationsListsService.findAllInstanceTypes(serviceContextRestExternal);

            // Transform
            InstanceTypes instanceTypes = do2RestExternalMapper.toInstanceTypes(entitiesResult);
            return instanceTypes;
        } catch (Exception e) {
            throw manageException(e);
        }
    }

    @Override
    public StatisticalOperationSources retrieveStatisticalOperationSources() {
        try {
            // Retrieve all
            List<SurveySource> entitiesResult = statisticalOperationsListsService.findAllSurveySources(serviceContextRestExternal);

            // Transform
            StatisticalOperationSources statisticalOperationSources = do2RestExternalMapper.toStatisticalOperationSources(entitiesResult);
            return statisticalOperationSources;
        } catch (Exception e) {
            throw manageException(e);
        }
    }

    @Override
    public CollMethods retrieveCollMethods() {
        try {
            // Retrieve all
            List<CollMethod> entitiesResult = statisticalOperationsListsService.findAllCollMethods(serviceContextRestExternal);

            // Transform
            CollMethods collMethods = do2RestExternalMapper.toCollMethods(entitiesResult);
            return collMethods;
        } catch (Exception e) {
            throw manageException(e);
        }
    }

    @Override
    public Costs retrieveCosts() {
        try {
            // Retrieve all
            List<Cost> entitiesResult = statisticalOperationsListsService.findAllCosts(serviceContextRestExternal);

            // Transform
            Costs costs = do2RestExternalMapper.toCosts(entitiesResult);
            return costs;
        } catch (Exception e) {
            throw manageException(e);
        }
    }

    /**
     * Retrieve operation by code (id in Api) and check it is published externally
     */
    private org.siemac.metamac.statistical.operations.core.domain.Operation retrieveOperationEntityPublishedExternally(String id) throws MetamacException {
        org.siemac.metamac.statistical.operations.core.domain.Operation operationEntity = statisticalOperationsBaseService.findOperationByCode(serviceContextRestExternal, id);
        if (operationEntity == null || !ProcStatusEnum.PUBLISH_EXTERNALLY.equals(operationEntity.getProcStatus())) {
            // Operation not found
            org.siemac.metamac.rest.common.v1_0.domain.Exception exception = RestExceptionUtils.getException(RestServiceExceptionType.OPERATION_NOT_FOUND, id);
            throw new RestException(exception, Status.NOT_FOUND);

        }
        return operationEntity;
    }

    /**
     * Retrieve family by code (id in Api) and check it is published externally
     */
    private org.siemac.metamac.statistical.operations.core.domain.Family retrieveFamilyEntityPublishedExternally(String id) throws MetamacException {
        org.siemac.metamac.statistical.operations.core.domain.Family familyEntity = statisticalOperationsBaseService.findFamilyByCode(serviceContextRestExternal, id);

        if (familyEntity == null || !ProcStatusEnum.PUBLISH_EXTERNALLY.equals(familyEntity.getProcStatus())) {
            // Family not found or not published externally
            org.siemac.metamac.rest.common.v1_0.domain.Exception exception = RestExceptionUtils.getException(RestServiceExceptionType.FAMILY_NOT_FOUND, id);
            throw new RestException(exception, Status.NOT_FOUND);

        }
        return familyEntity;
    }

    /**
     * Throws response error, logging exception
     */
    private RestException manageException(Exception e) {
        logger.error("Error", e);
        if (e instanceof RestException) {
            return (RestException) e;
        } else {
            // do not show information details about exception to user
            org.siemac.metamac.rest.common.v1_0.domain.Exception exception = RestExceptionUtils.getException(RestCommonServiceExceptionType.UNKNOWN);
            return new RestException(exception, Status.INTERNAL_SERVER_ERROR);
        }
    }
}