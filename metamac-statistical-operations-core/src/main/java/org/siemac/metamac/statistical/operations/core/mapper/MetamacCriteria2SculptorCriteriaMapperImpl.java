package org.siemac.metamac.statistical.operations.core.mapper;

import org.fornax.cartridges.sculptor.framework.domain.Property;
import org.siemac.metamac.core.common.criteria.MetamacCriteriaOrder;
import org.siemac.metamac.core.common.criteria.MetamacCriteriaPropertyRestriction;
import org.siemac.metamac.core.common.criteria.SculptorPropertyCriteria;
import org.siemac.metamac.core.common.criteria.mapper.MetamacCriteria2SculptorCriteria;
import org.siemac.metamac.core.common.criteria.mapper.MetamacCriteria2SculptorCriteria.CriteriaCallback;
import org.siemac.metamac.core.common.exception.MetamacException;
import org.siemac.metamac.statistical.operations.core.criteria.FamilyCriteriaPropertyEnum;
import org.siemac.metamac.statistical.operations.core.criteria.InstanceCriteriaPropertyEnum;
import org.siemac.metamac.statistical.operations.core.criteria.OperationCriteriaPropertyEnum;
import org.siemac.metamac.statistical.operations.core.domain.Family;
import org.siemac.metamac.statistical.operations.core.domain.FamilyProperties;
import org.siemac.metamac.statistical.operations.core.domain.Instance;
import org.siemac.metamac.statistical.operations.core.domain.InstanceProperties;
import org.siemac.metamac.statistical.operations.core.domain.Operation;
import org.siemac.metamac.statistical.operations.core.domain.OperationProperties;
import org.siemac.metamac.statistical.operations.core.error.ServiceExceptionType;
import org.springframework.stereotype.Component;

@Component
public class MetamacCriteria2SculptorCriteriaMapperImpl implements MetamacCriteria2SculptorCriteriaMapper {

    private MetamacCriteria2SculptorCriteria<Family>    familyCriteriaMapper    = null;
    private MetamacCriteria2SculptorCriteria<Operation> operationCriteriaMapper = null;
    private MetamacCriteria2SculptorCriteria<Instance>  instanceCriteriaMapper  = null;

    /**************************************************************************
     * Constructor
     **************************************************************************/

    public MetamacCriteria2SculptorCriteriaMapperImpl() throws MetamacException {
        familyCriteriaMapper = new MetamacCriteria2SculptorCriteria<Family>(Family.class, null, FamilyCriteriaPropertyEnum.class, new FamilyCriteriaCallback());
        operationCriteriaMapper = new MetamacCriteria2SculptorCriteria<Operation>(Operation.class, null, OperationCriteriaPropertyEnum.class, new OperationCriteriaCallback());
        instanceCriteriaMapper = new MetamacCriteria2SculptorCriteria<Instance>(Instance.class, null, InstanceCriteriaPropertyEnum.class, new InstanceCriteriaCallback());
    }

    /**************************************************************************
     * Mappings
     **************************************************************************/

    @Override
    public MetamacCriteria2SculptorCriteria<Family> getFamilyCriteriaMapper() {
        return familyCriteriaMapper;
    }

    @Override
    public MetamacCriteria2SculptorCriteria<Operation> getOperationCriteriaMapper() {
        return operationCriteriaMapper;
    }

    @Override
    public MetamacCriteria2SculptorCriteria<Instance> getInstanceCriteriaMapper() {
        return instanceCriteriaMapper;
    }

    /**************************************************************************
     * CALLBACK CLASSES
     **************************************************************************/

    private class FamilyCriteriaCallback implements CriteriaCallback {

        @Override
        public SculptorPropertyCriteria retrieveProperty(MetamacCriteriaPropertyRestriction propertyRestriction) throws MetamacException {
            FamilyCriteriaPropertyEnum propertyNameCriteria = FamilyCriteriaPropertyEnum.fromValue(propertyRestriction.getPropertyName());
            switch (propertyNameCriteria) {
                case CODE:
                    return new SculptorPropertyCriteria(FamilyProperties.code(), propertyRestriction.getStringValue());
                case TITLE:
                    return new SculptorPropertyCriteria(FamilyProperties.title().texts().label(), propertyRestriction.getStringValue());
                case ACRONYM:
                    return new SculptorPropertyCriteria(FamilyProperties.acronym().texts().label(), propertyRestriction.getStringValue());
                case DESCRIPTION:
                    return new SculptorPropertyCriteria(FamilyProperties.description().texts().label(), propertyRestriction.getStringValue());
                case PROC_STATUS:
                    return new SculptorPropertyCriteria(FamilyProperties.procStatus(), propertyRestriction.getEnumValue());
                case OPERATION_CODE:
                    return new SculptorPropertyCriteria(FamilyProperties.operations().code(), propertyRestriction.getStringValue());
                case OPERATION_ID:
                    return new SculptorPropertyCriteria(FamilyProperties.operations().id(), propertyRestriction.getLongValue());
                default:
                    throw new MetamacException(ServiceExceptionType.PARAMETER_INCORRECT, propertyRestriction.getPropertyName());
            }
        }

        @Override
        public Property retrievePropertyOrder(MetamacCriteriaOrder order) throws MetamacException {
            return null; // put default order
        }

        @Override
        public Property retrievePropertyOrderDefault() throws MetamacException {
            return FamilyProperties.id();
        }

    }

    private class OperationCriteriaCallback implements CriteriaCallback {

        @Override
        public SculptorPropertyCriteria retrieveProperty(MetamacCriteriaPropertyRestriction propertyRestriction) throws MetamacException {
            OperationCriteriaPropertyEnum propertyNameCriteria = OperationCriteriaPropertyEnum.fromValue(propertyRestriction.getPropertyName());
            switch (propertyNameCriteria) {
                case CODE:
                    return new SculptorPropertyCriteria(OperationProperties.code(), propertyRestriction.getStringValue());
                case TITLE:
                    return new SculptorPropertyCriteria(OperationProperties.title().texts().label(), propertyRestriction.getStringValue());
                case ACRONYM:
                    return new SculptorPropertyCriteria(OperationProperties.acronym().texts().label(), propertyRestriction.getStringValue());
                case DESCRIPTION:
                    return new SculptorPropertyCriteria(OperationProperties.description().texts().label(), propertyRestriction.getStringValue());
                case PROC_STATUS:
                    return new SculptorPropertyCriteria(OperationProperties.procStatus(), propertyRestriction.getEnumValue());
                case FAMILY_CODE:
                    return new SculptorPropertyCriteria(OperationProperties.families().code(), propertyRestriction.getStringValue());
                case FAMILY_ID:
                    return new SculptorPropertyCriteria(OperationProperties.families().id(), propertyRestriction.getLongValue());
                default:
                    throw new MetamacException(ServiceExceptionType.PARAMETER_INCORRECT, propertyRestriction.getPropertyName());
            }
        }

        @Override
        public Property retrievePropertyOrder(MetamacCriteriaOrder order) throws MetamacException {
            return null; // put default order
        }

        @Override
        public Property retrievePropertyOrderDefault() throws MetamacException {
            return OperationProperties.id();
        }

    }

    private class InstanceCriteriaCallback implements CriteriaCallback {

        @Override
        public SculptorPropertyCriteria retrieveProperty(MetamacCriteriaPropertyRestriction propertyRestriction) throws MetamacException {
            InstanceCriteriaPropertyEnum propertyNameCriteria = InstanceCriteriaPropertyEnum.fromValue(propertyRestriction.getPropertyName());
            switch (propertyNameCriteria) {
                case CODE:
                    return new SculptorPropertyCriteria(InstanceProperties.code(), propertyRestriction.getStringValue());
                case TITLE:
                    return new SculptorPropertyCriteria(InstanceProperties.title().texts().label(), propertyRestriction.getStringValue());
                case ACRONYM:
                    return new SculptorPropertyCriteria(InstanceProperties.acronym().texts().label(), propertyRestriction.getStringValue());
                case DATA_DESCRIPTION:
                    return new SculptorPropertyCriteria(InstanceProperties.dataDescription().texts().label(), propertyRestriction.getStringValue());
                case PROC_STATUS:
                    return new SculptorPropertyCriteria(InstanceProperties.procStatus(), propertyRestriction.getEnumValue());
                case OPERATION_CODE:
                    return new SculptorPropertyCriteria(InstanceProperties.operation().code(), propertyRestriction.getStringValue());
                case OPERATION_ID:
                    return new SculptorPropertyCriteria(InstanceProperties.operation().id(), propertyRestriction.getLongValue());
                default:
                    throw new MetamacException(ServiceExceptionType.PARAMETER_INCORRECT, propertyRestriction.getPropertyName());
            }
        }

        @Override
        public Property retrievePropertyOrder(MetamacCriteriaOrder order) throws MetamacException {
            return null; // put default order
        }

        @Override
        public Property retrievePropertyOrderDefault() throws MetamacException {
            return InstanceProperties.id();
        }

    }

}
